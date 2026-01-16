# 学术期刊配色推荐器

用R语言shiny包写了一个小程序，便于学术论文绘图时，离散配色方案的选择和参考，该程序已部署至云端，欢迎访问：<a href="http://lcpmgh.com/colors/" target="_blank">科研绘图配色推荐器</a>

我的服务器性能太差了，经常跑满内存储存，所以设定了每4个小时（即北京时间0、4、8、12、16、20点）重启服务器，如云端app无法访问，请下载至本地运行，或等待服务器在临近时点重启后访问，感谢理解😀。

 程序中包含的配色方案（保存于@colors.txt），收集自以下文章，在此表示感谢：

1. [跟着顶刊学科研绘图——science配色篇（一）](https://zhuanlan.zhihu.com/p/679737536)

2. [跟着顶刊学科研绘图——nature配色篇（一）](https://zhuanlan.zhihu.com/p/679425898)
3. [跟着顶刊学科研绘图——nature配色篇（二）](https://zhuanlan.zhihu.com/p/679572970)
4. [跟着顶刊学科研绘图——nature配色篇（三）](https://zhuanlan.zhihu.com/p/680011321)
5. [分享 | 顶刊高质量论文插图配色（含RGB值及16进制HEX码）（第一期）](https://zhuanlan.zhihu.com/p/670396774)
6. [分享 | 顶刊高质量论文插图配色（含RGB值及16进制HEX码）（第二期）](https://zhuanlan.zhihu.com/p/674906660)
7. [分享 | 顶刊高质量论文插图配色（含RGB值及16进制HEX码）（第三期）](https://zhuanlan.zhihu.com/p/689959306)
8. [论文配色：跟着顶刊学配色（Nature篇）](https://zhuanlan.zhihu.com/p/704390338)



shinyapp预览：

![shiny_app_preview](https://raw.githubusercontent.com/lcpmgh/colors/master/preview.png)



## 更新日志：

2026-01-16 v3.0

1. 优化代码，尤其是计算和展示流程，精简掉一些没用功能
2. 表格部分，用DT包替代reactable包，后者虽然在样式细节方面，参数丰富，但数据量大时会非常耗时
3. 经调整后，程序打开速度提升了64.35%

2024-10-08 v2.0

1. 颜色数据库：删除重复值、无效值（获取色值时ocr识别错误）
2. 数据库详情表中添加一列预览颜色，更直观，但展示出来更耗时
3. 删除随机显示，更改为，筛选颜色数量后，点击上下项或点击数据库展示绘图效果
4. 20250128，添加颜色透明度调节项

2024-04-05 v1.0

1. 创建程序
